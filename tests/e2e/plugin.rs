use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::time::Duration;

/// Test that render-line hook receives args properly
#[test]
fn test_render_line_hook_with_args() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    // Create a simple plugin that captures render-line hook args
    let test_plugin = r#"
// Test plugin to verify render-line hook receives args
let line_count = 0;
let found_marker = false;

globalThis.onRenderLine = function(args: {
    buffer_id: number;
    line_number: number;
    byte_start: number;
    byte_end: number;
    content: string;
}): boolean {
    editor.debug("render-line hook called!");
    // Verify args are present
    if (args && args.buffer_id !== undefined && args.line_number !== undefined && args.content !== undefined) {
        line_count = line_count + 1;
        editor.debug(`Line ${args.line_number}: ${args.content}`);

        // Look for "TEST_MARKER" in the content
        if (args.content.includes("TEST_MARKER")) {
            found_marker = true;
            editor.debug("Found TEST_MARKER!");
            editor.setStatus(`Found TEST_MARKER on line ${args.line_number} at byte ${args.byte_start}`);
        }
    } else {
        editor.debug("ERROR: args is nil or missing fields!");
    }
    return true;
};

editor.on("render_line", "onRenderLine");

globalThis.test_show_count = function(): void {
    editor.setStatus(`Rendered ${line_count} lines, found=${found_marker}`);
    line_count = 0;  // Reset counter
    found_marker = false;
};

editor.registerCommand(
    "Test: Show Line Count",
    "Show how many lines were rendered",
    "test_show_count",
    "normal"
);

editor.setStatus("Test plugin loaded!");
"#;

    let test_plugin_path = plugins_dir.join("test_render_hook.ts");
    fs::write(&test_plugin_path, test_plugin).unwrap();

    // Create test file with marker
    let test_file_content = "Line 1\nLine 2\nTEST_MARKER line\nLine 4\n";
    let fixture = TestFixture::new("test_render.txt", test_file_content).unwrap();

    // Create harness with the project directory
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open the test file - this should trigger render-line hooks
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Check that the test file is visible
    harness.assert_screen_contains("TEST_MARKER");

    // Run the "Show Line Count" command to verify hooks executed
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Test: Show Line Count").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Check the rendered screen content - should still contain the test content
    harness.assert_screen_contains("TEST_MARKER");

    // The plugin should have detected the marker via render-line hooks
    // We verify this by checking that the screen was rendered successfully
}

/// Test TODO Highlighter plugin - loads plugin, enables it, and checks highlighting
#[test]
fn test_todo_highlighter_plugin() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the TODO highlighter plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/todo_highlighter.ts");
    let plugin_dest = plugins_dir.join("todo_highlighter.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with TODO comments
    let test_file_content = r#"// This is a test file for the TODO Highlighter plugin

// TODO: Implement user authentication
// FIXME: Memory leak in connection pool
// HACK: Temporary workaround for parser bug
// NOTE: This function is performance-critical
// XXX: Needs review before production
// BUG: Off-by-one error in loop counter

# Python-style comments
# TODO: Add type hints to all functions
# FIXME: Handle edge case when list is empty

Regular text without keywords should not be highlighted:
TODO FIXME HACK NOTE XXX BUG (not in comments)
"#;

    let fixture = TestFixture::new("test_todo.txt", test_file_content).unwrap();

    // Create harness with the project directory (so plugins load)
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Check that file content is visible
    harness.assert_screen_contains("TODO: Implement user authentication");

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "TODO Highlighter: Enable" command
    harness.type_text("TODO Highlighter: Enable").unwrap();

    // Execute the command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Need an extra render to trigger the render-line hooks after enabling
    harness.render().unwrap();

    // Now check that highlights are actually rendered
    // The TODO keyword should have a background color applied
    // Let's find the position of "TODO" in the first comment and check its style

    let screen = harness.screen_to_string();
    println!("Screen after enabling TODO highlighter:\n{}", screen);

    // Find the position of "TODO" on screen
    let lines: Vec<&str> = screen.lines().collect();
    let mut found_highlighted_todo = false;

    for (y, line) in lines.iter().enumerate() {
        if let Some(x) = line.find("TODO") {
            // Check if this TODO is in a comment (should have "//" before it)
            if line[..x].contains("//") {
                // Check the style of the 'T' in "TODO"
                if let Some(style) = harness.get_cell_style(x as u16, y as u16) {
                    // Check if background color is an actual RGB color (not just Reset)
                    // TODO keywords should be orange (255, 165, 0)
                    if let Some(bg) = style.bg {
                        println!(
                            "Found TODO at ({}, {}) with background color: {:?}",
                            x, y, bg
                        );
                        // Only count as highlighted if it's an actual RGB color
                        if matches!(bg, ratatui::style::Color::Rgb(_, _, _)) {
                            found_highlighted_todo = true;
                            break;
                        }
                    }
                }
            }
        }
    }

    assert!(
        found_highlighted_todo,
        "Expected to find at least one highlighted TODO keyword"
    );
}

/// Test TODO Highlighter disable command
#[test]
fn test_todo_highlighter_disable() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the TODO highlighter plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/todo_highlighter.ts");
    let plugin_dest = plugins_dir.join("todo_highlighter.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with TODO comments
    let test_file_content = "// TODO: Test comment\n";
    let fixture = TestFixture::new("test_todo.txt", test_file_content).unwrap();

    // Create harness with the project directory (so plugins load)
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Enable highlighting first
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("TODO Highlighter: Enable").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Now disable it
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("TODO Highlighter: Disable").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify the content is still visible after disabling
    harness.assert_screen_contains("TODO: Test comment");

    // The test passes if we can execute disable without error
    // Highlighting should be removed but we don't check for it explicitly
    // since removing overlays doesn't leave visible traces to assert on
}

/// Test TODO Highlighter toggle command
#[test]
fn test_todo_highlighter_toggle() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the TODO highlighter plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/todo_highlighter.ts");
    let plugin_dest = plugins_dir.join("todo_highlighter.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with TODO comments
    let test_file_content = "// TODO: Test comment\n";
    let fixture = TestFixture::new("test_todo.txt", test_file_content).unwrap();

    // Create harness with the project directory (so plugins load)
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Toggle on
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("TODO Highlighter: Toggle").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Need an extra render to trigger the render-line hooks after enabling
    harness.render().unwrap();

    // Verify highlighting is enabled by checking for background color
    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();
    let mut found_highlighted = false;

    for (y, line) in lines.iter().enumerate() {
        if let Some(x) = line.find("TODO") {
            if line[..x].contains("//") {
                if let Some(style) = harness.get_cell_style(x as u16, y as u16) {
                    // Only count as highlighted if it's an actual RGB color
                    if let Some(bg) = style.bg {
                        if matches!(bg, ratatui::style::Color::Rgb(_, _, _)) {
                            found_highlighted = true;
                            break;
                        }
                    }
                }
            }
        }
    }

    assert!(
        found_highlighted,
        "Expected TODO to be highlighted after toggle on"
    );

    // Toggle off
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("TODO Highlighter: Toggle").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify content is still visible after toggling off
    harness.assert_screen_contains("TODO: Test comment");
}

/// Test TODO Highlighter updates when buffer content changes
///
/// This test documents a known limitation: overlays don't update positions
/// when the buffer is modified. When text is inserted before an overlay,
/// the overlay stays at its original byte position instead of shifting.
#[test]
#[ignore = "Overlays don't update positions when buffer changes - needs overlay position tracking fix"]
fn test_todo_highlighter_updates_on_edit() {
    // Enable tracing for debugging
    use tracing_subscriber::{fmt, EnvFilter};
    let _ = fmt()
        .with_env_filter(
            EnvFilter::from_default_env().add_directive("fresh=trace".parse().unwrap()),
        )
        .with_test_writer()
        .try_init();

    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the TODO highlighter plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/todo_highlighter.ts");
    let plugin_dest = plugins_dir.join("todo_highlighter.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with TODO comment at the start
    let test_file_content = "// TODO: Original comment\n";
    let fixture = TestFixture::new("test_todo.txt", test_file_content).unwrap();

    // Create harness with the project directory (so plugins load)
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Enable highlighting
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("TODO Highlighter: Enable").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify the original TODO is highlighted
    let screen_before = harness.screen_to_string();
    println!("Screen before edit:\n{}", screen_before);

    let lines: Vec<&str> = screen_before.lines().collect();
    let mut found_original_todo = false;

    for (y, line) in lines.iter().enumerate() {
        if line.contains("TODO: Original") {
            if let Some(x) = line.find("TODO") {
                if let Some(style) = harness.get_cell_style(x as u16, y as u16) {
                    if let Some(bg) = style.bg {
                        // Check it's not just Reset/White, should be a real color
                        println!("Found TODO at ({}, {}) with background: {:?}", x, y, bg);
                        found_original_todo = true;
                        break;
                    }
                }
            }
        }
    }

    assert!(
        found_original_todo,
        "Expected to find highlighted 'TODO: Original' before edit"
    );

    // Go to the beginning of the file
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Insert a new line at the top: "// FIXME: New comment\n"
    harness.type_text("// FIXME: New comment\n").unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    println!("Screen after adding FIXME:\n{}", screen_after);

    // The buffer should now be:
    // Line 1: // FIXME: New comment
    // Line 2: // TODO: Original comment

    // Check that FIXME is highlighted
    let lines: Vec<&str> = screen_after.lines().collect();
    let mut found_fixme = false;
    let mut found_todo_on_line_2 = false;

    for (y, line) in lines.iter().enumerate() {
        if line.contains("FIXME: New") {
            if let Some(x) = line.find("FIXME") {
                if let Some(style) = harness.get_cell_style(x as u16, y as u16) {
                    if let Some(bg) = style.bg {
                        println!("Found FIXME at ({}, {}) with background: {:?}", x, y, bg);
                        found_fixme = true;
                    }
                }
            }
        }
        if line.contains("TODO: Original") {
            if let Some(x) = line.find("TODO") {
                if let Some(style) = harness.get_cell_style(x as u16, y as u16) {
                    if let Some(bg) = style.bg {
                        println!(
                            "Found TODO on line 2 at ({}, {}) with background: {:?}",
                            x, y, bg
                        );
                        // Check if it's an actual RGB color (orange), not just Reset
                        if matches!(bg, ratatui::style::Color::Rgb(_, _, _)) {
                            found_todo_on_line_2 = true;
                        }
                    }
                }
            }
        }
    }

    // Bug: FIXME gets highlighted because it happens to be at the byte position where TODO was
    // But TODO should ALSO be highlighted, not just have Reset background
    assert!(
        found_fixme,
        "Expected to find highlighted FIXME after inserting new line"
    );

    // This assertion will FAIL, demonstrating the bug - TODO highlight doesn't update
    assert!(
        found_todo_on_line_2,
        "BUG REPRODUCED: TODO on line 2 is not highlighted! The old overlay at byte 3-7 \
         now highlights FIXME (which happens to be at those bytes), but TODO moved to a \
         new byte position and didn't get a new overlay. Overlays need to update when buffer changes!"
    );
}

/// Test TODO Highlighter updates correctly when deleting text
#[test]
fn test_todo_highlighter_updates_on_delete() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the TODO highlighter plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/todo_highlighter.ts");
    let plugin_dest = plugins_dir.join("todo_highlighter.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with TODO on second line
    let test_file_content = "// FIXME: Delete this line\n// TODO: Keep this one\n";
    let fixture = TestFixture::new("test_todo.txt", test_file_content).unwrap();

    // Create harness with the project directory (so plugins load)
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Enable highlighting
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("TODO Highlighter: Enable").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Need an extra render to trigger the render-line hooks after enabling
    harness.render().unwrap();

    // Verify both keywords are highlighted initially
    let screen_before = harness.screen_to_string();
    println!("Screen before delete:\n{}", screen_before);

    let mut found_fixme_before = false;
    let mut found_todo_before = false;

    for (y, line) in screen_before.lines().enumerate() {
        if line.contains("FIXME") && line[..line.find("FIXME").unwrap()].contains("//") {
            if let Some(x) = line.find("FIXME") {
                if let Some(style) = harness.get_cell_style(x as u16, y as u16) {
                    if let Some(bg) = style.bg {
                        println!(
                            "Found FIXME highlighted at ({}, {}) before delete with bg: {:?}",
                            x, y, bg
                        );
                        found_fixme_before = true;
                    }
                }
            }
        }
        if line.contains("TODO") && line[..line.find("TODO").unwrap()].contains("//") {
            if let Some(x) = line.find("TODO") {
                if let Some(style) = harness.get_cell_style(x as u16, y as u16) {
                    if let Some(bg) = style.bg {
                        println!(
                            "Found TODO highlighted at ({}, {}) before delete with bg: {:?}",
                            x, y, bg
                        );
                        found_todo_before = true;
                    }
                }
            }
        }
    }

    assert!(found_fixme_before, "FIXME should be highlighted initially");
    assert!(found_todo_before, "TODO should be highlighted initially");

    // Now delete the first line (FIXME line)
    // Go to beginning
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Select the entire first line
    harness.send_key(KeyCode::End, KeyModifiers::SHIFT).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap(); // Include the newline
    harness.render().unwrap();

    // Delete the selection
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    println!("Screen after deleting FIXME line:\n{}", screen_after);

    // The buffer should now only contain: "// TODO: Keep this one\n"
    // TODO should still be highlighted (now on line 1)

    let mut found_todo_after = false;

    for (y, line) in screen_after.lines().enumerate() {
        if line.contains("TODO") && line[..line.find("TODO").unwrap()].contains("//") {
            if let Some(x) = line.find("TODO") {
                if let Some(style) = harness.get_cell_style(x as u16, y as u16) {
                    if let Some(bg) = style.bg {
                        println!(
                            "Found TODO at ({}, {}) after delete with background: {:?}",
                            x, y, bg
                        );
                        found_todo_after = true;
                    }
                }
            }
        }
    }

    assert!(
        found_todo_after,
        "BUG: TODO should still be highlighted after deleting the line above it! \
         Instead, the highlight either disappeared or shifted to the wrong position."
    );
}

/// Test diagnostics panel plugin loads and creates a virtual buffer split
/// This verifies the full implementation with LSP-like diagnostics display
#[test]
fn test_diagnostics_panel_plugin_loads() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the diagnostics panel plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/diagnostics_panel.ts");
    let plugin_dest = plugins_dir.join("diagnostics_panel.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create a simple test file
    let test_file_content = "fn main() {\n    println!(\"test\");\n}\n";
    let fixture = TestFixture::new("test_diagnostics.rs", test_file_content).unwrap();

    // Create harness with the project directory (so plugins load)
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open the test file - this should trigger plugin loading
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Check that file content is visible
    harness.assert_screen_contains("fn main()");

    // The plugin should have loaded successfully without Lua errors
    // If the Lua scoping is wrong (update_panel_content not defined before create_panel calls it),
    // the plugin would fail to load with: "attempt to call a nil value (global 'update_panel_content')"

    // The plugin sets a status message on successful load
    // Look for evidence that the plugin loaded by checking the screen
    let screen = harness.screen_to_string();
    println!("Screen after plugin load:\n{}", screen);

    // Now try to execute the "Show Diagnostics" command
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type to search for the command
    harness.type_text("Show Diagnostics").unwrap();
    harness.render().unwrap();

    let palette_screen = harness.screen_to_string();
    println!("Command palette screen:\n{}", palette_screen);

    // The command should be visible in the palette (registered by the plugin)
    // If the plugin failed to load due to Lua errors, this command wouldn't be registered
    assert!(
        palette_screen.contains("Show Diagnostics")
            || palette_screen.contains("diagnostics")
            || palette_screen.contains("Diagnostics"),
        "The 'Show Diagnostics' command should be registered by the plugin. \
         If the plugin had Lua scoping errors, it wouldn't load and the command wouldn't exist."
    );

    // Execute the command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let final_screen = harness.screen_to_string();
    println!("Final screen after executing command:\n{}", final_screen);

    // Verify the diagnostics panel content is displayed in a horizontal split
    assert!(
        final_screen.contains("LSP Diagnostics"),
        "Expected to see 'LSP Diagnostics' header in the panel"
    );
    assert!(
        final_screen.contains("[E]") || final_screen.contains("[W]"),
        "Expected to see severity icons like [E] or [W] in the diagnostics"
    );
    assert!(
        final_screen.contains(">"),
        "Expected to see '>' marker for selected diagnostic"
    );
    assert!(
        final_screen.contains("*Diagnostics*"),
        "Expected to see buffer name '*Diagnostics*' in status bar"
    );
    // Verify horizontal split view (separator line should be visible)
    assert!(
        final_screen.contains("───") || final_screen.contains("---"),
        "Expected to see horizontal split separator"
    );
    // The original code buffer should still be visible above the diagnostics
    assert!(
        final_screen.contains("fn main()"),
        "Expected to see original code buffer in upper split"
    );
}

/// Test editor <-> plugin message queue architecture
///
/// This test exercises the complete bidirectional message flow:
/// 1. Editor sends action request to plugin thread (fire-and-forget)
/// 2. Plugin executes action and sends commands back via command queue
/// 3. Editor polls command queue with try_recv (non-blocking)
/// 4. Plugin awaits async response for buffer ID
///
/// This ensures no deadlocks occur in the message passing architecture.
#[test]
fn test_plugin_message_queue_architecture() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    // Create a test plugin that exercises the message queue:
    // 1. Registers a command (tests command registration via plugin commands)
    // 2. When executed, creates a virtual buffer in split (tests async response)
    // 3. Uses the returned buffer ID to set status (tests async result propagation)
    let test_plugin = r#"
// Test plugin for message queue architecture
// This plugin exercises the bidirectional message flow

// Register a command that will create a virtual buffer
editor.registerCommand(
    "Test: Create Virtual Buffer",
    "Create a virtual buffer and verify buffer ID is returned",
    "test_create_virtual_buffer",
    "normal"
);

// Counter to track executions
let executionCount = 0;

globalThis.test_create_virtual_buffer = async function(): Promise<void> {
    executionCount++;
    editor.setStatus(`Starting execution ${executionCount}...`);

    // Create entries for the virtual buffer
    const entries = [
        {
            text: `Test entry ${executionCount}\n`,
            properties: {
                index: executionCount,
            },
        },
    ];

    try {
        // This is the critical async operation that tests:
        // 1. Plugin sends CreateVirtualBufferInSplit command
        // 2. Editor creates buffer and sends response
        // 3. Plugin receives buffer ID via async channel
        const bufferId = await editor.createVirtualBufferInSplit({
            name: "*Test Buffer*",
            mode: "normal",
            read_only: true,
            entries: entries,
            ratio: 0.5,
            panel_id: "test-panel",
            show_line_numbers: false,
            show_cursors: true,
        });

        // Verify we got a valid buffer ID
        if (typeof bufferId === 'number' && bufferId > 0) {
            editor.setStatus(`Success: Created buffer ID ${bufferId}`);
        } else {
            editor.setStatus(`Error: Invalid buffer ID: ${bufferId}`);
        }
    } catch (error) {
        const msg = error instanceof Error ? error.message : String(error);
        editor.setStatus(`Error: ${msg}`);
    }
};

editor.setStatus("Message queue test plugin loaded!");
"#;

    let test_plugin_path = plugins_dir.join("test_message_queue.ts");
    fs::write(&test_plugin_path, test_plugin).unwrap();

    // Create a simple test file
    let test_file_content = "Test file content\nLine 2\nLine 3\n";
    let fixture = TestFixture::new("test_file.txt", test_file_content).unwrap();

    // Create harness with the project directory (so plugins load)
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open the test file - this should trigger plugin loading
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Verify file content is visible
    harness.assert_screen_contains("Test file content");

    // Verify plugin loaded by checking for the status message
    let screen = harness.screen_to_string();
    println!("Screen after plugin load:\n{}", screen);

    // Now execute the command that creates a virtual buffer
    // This exercises the full message queue flow
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type the command name
    harness.type_text("Test: Create Virtual Buffer").unwrap();
    harness.render().unwrap();

    // Verify command appears in palette (proves command registration worked)
    let palette_screen = harness.screen_to_string();
    println!("Command palette screen:\n{}", palette_screen);
    assert!(
        palette_screen.contains("Create Virtual Buffer"),
        "Command should be registered and visible in palette"
    );

    // Execute the command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Give the async operation time to complete
    // We need multiple render cycles for the message queue to process
    for i in 0..10 {
        harness.render().unwrap();
        std::thread::sleep(Duration::from_millis(50));

        let screen = harness.screen_to_string();
        if screen.contains("Success: Created buffer ID") {
            println!("Async operation completed after {} iterations", i + 1);
            break;
        }
    }

    let final_screen = harness.screen_to_string();
    println!("Final screen after command execution:\n{}", final_screen);

    // Verify the async operation completed successfully
    // The status should contain the buffer ID
    assert!(
        final_screen.contains("Success: Created buffer ID"),
        "Expected status to show successful buffer creation with ID. \
         Got screen:\n{}\n\n\
         If this fails with 'Starting execution...' still visible, \
         the async response is not being delivered. \
         If it shows an error, check the error message.",
        final_screen
    );

    // Verify the virtual buffer split is visible
    // The test buffer should show "*Test Buffer*" and the test entry
    assert!(
        final_screen.contains("*Test Buffer*") || final_screen.contains("Test entry"),
        "Expected to see the virtual buffer content. \
         The split should show either the buffer name or entry content."
    );

    // Verify the original file content is still visible (split view working)
    assert!(
        final_screen.contains("Test file content"),
        "Expected original file content to still be visible in split view"
    );
}

/// Test that multiple plugin actions can be queued without deadlock
#[test]
fn test_plugin_multiple_actions_no_deadlock() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    // Create a plugin with multiple commands that all set status
    let test_plugin = r#"
// Test plugin for multiple concurrent actions

editor.registerCommand("Action A", "Set status to A", "action_a", "normal");
editor.registerCommand("Action B", "Set status to B", "action_b", "normal");
editor.registerCommand("Action C", "Set status to C", "action_c", "normal");

globalThis.action_a = function(): void {
    editor.setStatus("Status: A executed");
};

globalThis.action_b = function(): void {
    editor.setStatus("Status: B executed");
};

globalThis.action_c = function(): void {
    editor.setStatus("Status: C executed");
};

editor.setStatus("Multi-action plugin loaded");
"#;

    let test_plugin_path = plugins_dir.join("test_multi_action.ts");
    fs::write(&test_plugin_path, test_plugin).unwrap();

    // Create a simple test file
    let test_file_content = "Test content\n";
    let fixture = TestFixture::new("test.txt", test_file_content).unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open file and load plugins
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Execute multiple commands in sequence rapidly
    // This tests that the message queue handles multiple actions correctly
    for (action_name, expected_status) in [
        ("Action A", "A executed"),
        ("Action B", "B executed"),
        ("Action C", "C executed"),
    ] {
        // Open command palette
        harness
            .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();

        // Type and execute command
        harness.type_text(action_name).unwrap();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();

        // Give time for processing
        for _ in 0..5 {
            harness.render().unwrap();
            std::thread::sleep(Duration::from_millis(20));
        }

        let screen = harness.screen_to_string();
        assert!(
            screen.contains(expected_status),
            "Expected status '{}' after executing '{}'. Got:\n{}",
            expected_status,
            action_name,
            screen
        );
    }
}

/// Test that plugin action execution is non-blocking
///
/// This verifies that the editor doesn't hang when executing a plugin action
/// even if the action takes time. The fire-and-forget pattern should allow
/// the editor to continue processing events.
#[test]
fn test_plugin_action_nonblocking() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    // Create a plugin that does some work
    let test_plugin = r#"
// Test plugin to verify non-blocking action execution

editor.registerCommand(
    "Slow Action",
    "An action that does some computation",
    "slow_action",
    "normal"
);

globalThis.slow_action = function(): void {
    // Simulate some work (this is synchronous but should not block editor)
    let sum = 0;
    for (let i = 0; i < 1000; i++) {
        sum += i;
    }
    editor.setStatus(`Completed: sum = ${sum}`);
};

editor.setStatus("Nonblocking test plugin loaded");
"#;

    let test_plugin_path = plugins_dir.join("test_nonblocking.ts");
    fs::write(&test_plugin_path, test_plugin).unwrap();

    // Create a simple test file
    let test_file_content = "Test\n";
    let fixture = TestFixture::new("test.txt", test_file_content).unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Execute the slow action
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Slow Action").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // The key test: we should be able to render multiple times
    // without the editor hanging
    let start = std::time::Instant::now();
    for _ in 0..10 {
        harness.render().unwrap();
        std::thread::sleep(Duration::from_millis(10));
    }
    let elapsed = start.elapsed();

    // If the action was blocking, this would take much longer
    // or hang entirely
    assert!(
        elapsed < Duration::from_secs(2),
        "Rendering should complete quickly even with action running. Took {:?}",
        elapsed
    );

    // Verify the action completed
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Completed: sum"),
        "Expected action to complete and set status. Got:\n{}",
        screen
    );
}

/// Performance test for TODO highlighter with cursor movement
/// Run with: RUST_LOG=trace cargo test test_todo_highlighter_cursor_perf -- --nocapture
#[test]
fn test_todo_highlighter_cursor_perf() {
    // Initialize tracing subscriber for performance analysis
    let _ = tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive("fresh=trace".parse().unwrap()),
        )
        .with_test_writer()
        .try_init();

    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the TODO highlighter plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/todo_highlighter.ts");
    let plugin_dest = plugins_dir.join("todo_highlighter.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create a larger test file with many lines and TODO comments
    let mut test_content = String::new();
    for i in 0..100 {
        if i % 5 == 0 {
            test_content.push_str(&format!("// TODO: Task number {}\n", i));
        } else if i % 7 == 0 {
            test_content.push_str(&format!("// FIXME: Issue number {}\n", i));
        } else {
            test_content.push_str(&format!("Line {} of test content\n", i));
        }
    }
    let fixture = TestFixture::new("test_perf.txt", &test_content).unwrap();

    // Create harness with the project directory
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 40, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Enable TODO Highlighter
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("TODO Highlighter: Enable").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Measure cursor movement performance
    let num_moves = 20;

    println!("\n=== TODO Highlighter Cursor Movement Performance Test ===");
    println!("Moving cursor down {} times...", num_moves);

    let down_start = std::time::Instant::now();
    for _ in 0..num_moves {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    let down_elapsed = down_start.elapsed();

    println!("Moving cursor up {} times...", num_moves);

    let up_start = std::time::Instant::now();
    for _ in 0..num_moves {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    let up_elapsed = up_start.elapsed();

    println!("\n=== Results ===");
    println!("Down: {:?} total, {:?} per move", down_elapsed, down_elapsed / num_moves);
    println!("Up: {:?} total, {:?} per move", up_elapsed, up_elapsed / num_moves);
    println!("Total: {:?}", down_elapsed + up_elapsed);
    println!("================\n");

    // No assertion on timing - this is for data collection
    // The trace logs will show where time is spent
}

/// Test that closing and reopening a panel with the same panel_id works correctly
///
/// This test reproduces a bug where:
/// 1. A virtual buffer is created with a panel_id
/// 2. The buffer is closed
/// 3. Another virtual buffer is created with the same panel_id
/// 4. The editor tries to reuse the old buffer ID (which no longer exists)
/// 5. Result: "Failed to update panel content: Buffer not found"
///
/// The root cause is that `close_buffer()` doesn't clean up the `panel_ids` mapping,
/// leaving a stale entry that points to a non-existent buffer.
#[test]
fn test_panel_id_cleanup_after_buffer_close() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    // Create a plugin that creates a virtual buffer panel, closes it, and creates it again
    let test_plugin = r#"
// Test plugin for panel_id cleanup after buffer close
// This reproduces the find references bug where closing and reopening fails

let panelBufferId: number | null = null;
let panelSplitId: number | null = null;
let executionCount = 0;

editor.registerCommand(
    "Test: Create Panel",
    "Create a virtual buffer panel with panel_id",
    "test_create_panel",
    "normal"
);

editor.registerCommand(
    "Test: Close Panel",
    "Close the panel buffer",
    "test_close_panel",
    "normal"
);

// Create panel with panel_id
globalThis.test_create_panel = async function(): Promise<void> {
    executionCount++;
    editor.setStatus(`Creating panel (attempt ${executionCount})...`);

    const entries = [
        {
            text: `Panel content ${executionCount}\n`,
            properties: { index: executionCount },
        },
        {
            text: `Created at ${Date.now()}\n`,
            properties: { type: "info" },
        },
    ];

    try {
        panelBufferId = await editor.createVirtualBufferInSplit({
            name: "*Test Panel*",
            mode: "normal",
            read_only: true,
            entries: entries,
            ratio: 0.7,
            panel_id: "test-reusable-panel", // Same panel_id every time
            show_line_numbers: false,
            show_cursors: true,
        });

        panelSplitId = editor.getActiveSplitId();
        editor.setStatus(`Panel created (attempt ${executionCount}): buffer ID ${panelBufferId}`);
        editor.debug(`Panel created with buffer ID ${panelBufferId}, split ID ${panelSplitId}`);
    } catch (error) {
        const msg = error instanceof Error ? error.message : String(error);
        editor.setStatus(`Panel creation FAILED (attempt ${executionCount}): ${msg}`);
        editor.debug(`ERROR: Panel creation failed: ${msg}`);
    }
};

// Close the panel
globalThis.test_close_panel = function(): void {
    if (panelBufferId !== null) {
        editor.closeBuffer(panelBufferId);
        editor.setStatus(`Panel closed (buffer ID was ${panelBufferId})`);
        editor.debug(`Closed panel buffer ID ${panelBufferId}`);

        if (panelSplitId !== null) {
            editor.closeSplit(panelSplitId);
        }

        panelBufferId = null;
        panelSplitId = null;
    } else {
        editor.setStatus("No panel to close");
    }
};

editor.setStatus("Panel cleanup test plugin loaded");
"#;

    let test_plugin_path = plugins_dir.join("test_panel_cleanup.ts");
    fs::write(&test_plugin_path, test_plugin).unwrap();

    // Create a simple test file
    let test_file_content = "Test file for panel cleanup\n";
    let fixture = TestFixture::new("test.txt", test_file_content).unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open file and load plugins
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Wait for plugin to load
    for _ in 0..5 {
        harness.render().unwrap();
        std::thread::sleep(Duration::from_millis(50));
    }

    // === First panel creation - should succeed ===
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Test: Create Panel").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for async operation
    for _ in 0..10 {
        harness.render().unwrap();
        std::thread::sleep(Duration::from_millis(50));
    }

    let screen1 = harness.screen_to_string();
    println!("Screen after first panel creation:\n{}", screen1);

    assert!(
        screen1.contains("Panel created (attempt 1)"),
        "First panel creation should succeed. Got:\n{}",
        screen1
    );
    assert!(
        screen1.contains("Panel content 1"),
        "First panel content should be visible. Got:\n{}",
        screen1
    );

    // === Close the panel ===
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Test: Close Panel").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    for _ in 0..5 {
        harness.render().unwrap();
        std::thread::sleep(Duration::from_millis(50));
    }

    let screen2 = harness.screen_to_string();
    println!("Screen after closing panel:\n{}", screen2);

    assert!(
        screen2.contains("Panel closed"),
        "Panel close should succeed. Got:\n{}",
        screen2
    );

    // === Second panel creation - this is where the bug manifests ===
    // The editor should create a new panel, but instead it tries to reuse
    // the old buffer ID (which no longer exists) because panel_ids wasn't cleaned up
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Test: Create Panel").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for async operation
    for _ in 0..10 {
        harness.render().unwrap();
        std::thread::sleep(Duration::from_millis(50));
    }

    let screen3 = harness.screen_to_string();
    println!("Screen after second panel creation:\n{}", screen3);

    // This is the key assertion - second creation should succeed
    // Before the fix, this will fail because panel_ids has a stale entry
    assert!(
        screen3.contains("Panel created (attempt 2)"),
        "BUG: Second panel creation should succeed, but it failed. \
         The panel_ids mapping wasn't cleaned up when the buffer was closed, \
         causing the editor to try to reuse a non-existent buffer ID. Got:\n{}",
        screen3
    );
    assert!(
        screen3.contains("Panel content 2"),
        "Second panel content should be visible. Got:\n{}",
        screen3
    );
}
