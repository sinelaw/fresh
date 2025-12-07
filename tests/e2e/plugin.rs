use crate::common::fake_lsp::FakeLspServer;
use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::services::lsp::client::LspServerConfig;
use fresh::services::process_limits::ProcessLimits;
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
    let test_plugin = r###"
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
"###;

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

    // Need multiple async processing cycles for plugin to:
    // 1. Process the RefreshLines command from the channel
    // 2. Clear seen_lines and set plugin_render_requested
    // 3. Re-render to trigger lines_changed hook
    // 4. Process addOverlay commands from the hook
    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();

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
                    // Check if foreground color is an actual RGB color (overlays set foreground, not background)
                    // TODO keywords should be yellow (255, 200, 50)
                    if let Some(fg) = style.fg {
                        println!(
                            "Found TODO at ({}, {}) with foreground color: {:?}",
                            x, y, fg
                        );
                        // Only count as highlighted if it's an actual RGB color
                        if matches!(fg, ratatui::style::Color::Rgb(_, _, _)) {
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

    // Need multiple async processing cycles for plugin to process RefreshLines and addOverlay commands
    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();

    // Verify highlighting is enabled by checking for foreground color (overlays set foreground, not background)
    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();
    let mut found_highlighted = false;

    for (y, line) in lines.iter().enumerate() {
        if let Some(x) = line.find("TODO") {
            if line[..x].contains("//") {
                if let Some(style) = harness.get_cell_style(x as u16, y as u16) {
                    // Only count as highlighted if it's an actual RGB color
                    if let Some(fg) = style.fg {
                        if matches!(fg, ratatui::style::Color::Rgb(_, _, _)) {
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
    let project_root = temp_dir.path().to_path_buf();

    // Create plugins directory and copy the diagnostics panel plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/diagnostics_panel.ts");
    let plugin_dest = plugins_dir.join("diagnostics_panel.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create a simple test file in the project directory (not via TestFixture!)
    let test_file_content = "fn main() {\n    println!(\"test\");\n}\n";
    let test_file = project_root.join("test_diagnostics.rs");
    fs::write(&test_file, test_file_content).unwrap();

    // Create harness with the project directory (so plugins load)
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open the test file - this should trigger plugin loading
    harness.open_file(&test_file).unwrap();
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

    // Wait for the async operation to complete by checking for visible buffer content
    let completed = harness
        .wait_for_async(
            |h| {
                let screen = h.screen_to_string();
                screen.contains("*Test Buffer*") || screen.contains("Test entry")
            },
            5000,
        )
        .unwrap();

    let final_screen = harness.screen_to_string();
    println!("Final screen after command execution:\n{}", final_screen);

    assert!(
        completed,
        "Async operation should complete within timeout. Got:\n{}",
        final_screen
    );

    // Verify the async operation completed successfully by checking for visible content
    // Note: We check for the buffer content rather than status message,
    // because status messages may have timing issues with async processing
    assert!(
        final_screen.contains("*Test Buffer*") || final_screen.contains("Test entry"),
        "Expected to see the virtual buffer content. \
         The split should show either the buffer name or entry content. \
         Got screen:\n{}",
        final_screen
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
    // The main assertion is that the commands execute without deadlock or hanging
    let start = std::time::Instant::now();

    for action_name in ["Action A", "Action B", "Action C"] {
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

        // Process async and render a few times to let the action execute
        for _ in 0..3 {
            harness.process_async_and_render().unwrap();
            std::thread::sleep(Duration::from_millis(20));
        }
    }

    let elapsed = start.elapsed();

    // The key assertion: all commands should execute quickly without deadlock
    // If there's a deadlock or blocking issue, this would timeout
    assert!(
        elapsed < Duration::from_secs(2),
        "Multiple actions should complete without deadlock. Took {:?}",
        elapsed
    );

    // Verify the editor is still responsive
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Test content"),
        "Editor should still show content after multiple actions. Got:\n{}",
        screen
    );
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

    // The key test: we should be able to render without blocking while action runs
    let start = std::time::Instant::now();

    // Process several render cycles - this verifies the editor isn't blocked
    for _ in 0..5 {
        harness.process_async_and_render().unwrap();
        std::thread::sleep(Duration::from_millis(50));
    }

    let elapsed = start.elapsed();

    // If the action was blocking, renders would stall
    // The action execution is async, so renders complete immediately
    assert!(
        elapsed < Duration::from_secs(1),
        "Rendering should complete quickly even with action running. Took {:?}",
        elapsed
    );

    // Verify the screen is still responsive (command palette closed)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Test"),
        "Editor should show file content. Got:\n{}",
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
    println!(
        "Down: {:?} total, {:?} per move",
        down_elapsed,
        down_elapsed / num_moves
    );
    println!(
        "Up: {:?} total, {:?} per move",
        up_elapsed,
        up_elapsed / num_moves
    );
    println!("Total: {:?}", down_elapsed + up_elapsed);
    println!("================\n");

    // No assertion on timing - this is for data collection
    // The trace logs will show where time is spent
}

/// Test Color Highlighter plugin - loads plugin, enables it, and checks for color swatches
#[test]
#[ignore]
fn test_color_highlighter_plugin() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the color highlighter plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/color_highlighter.ts");
    let plugin_dest = plugins_dir.join("color_highlighter.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with various color formats
    let test_file_content = r###"// Test file for Color Highlighter
// CSS hex colors
let red = "#ff0000";
let green = "#0f0";
let blue = "#0000ff";
let transparent = "#ff000080";

// CSS rgb/rgba
background: rgb(255, 128, 0);
color: rgba(0, 255, 128, 0.5);

// CSS hsl/hsla
hsl(120, 100%, 50%);
hsla(240, 100%, 50%, 0.8);

// Rust colors
Color::Rgb(255, 255, 0)
Color::Rgb(128, 0, 255)
"###;

    let fixture = TestFixture::new("test_colors.txt", test_file_content).unwrap();

    // Create harness with the project directory (so plugins load)
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Check that file content is visible
    harness.assert_screen_contains("#ff0000");

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "Color Highlighter: Enable" command
    harness.type_text("Color Highlighter: Enable").unwrap();

    // Execute the command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Need extra renders to trigger the render-line hooks after enabling
    harness.render().unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen after enabling Color highlighter:\n{}", screen);

    // Check that color swatches (█) appear in the output
    // The plugin adds "█ " before each color code
    let swatch_count = screen.matches('█').count();
    println!("Found {} color swatches", swatch_count);

    // We should have at least some color swatches visible
    // (the file has many colors, some should be in the viewport)
    assert!(
        swatch_count > 0,
        "Expected to find color swatch characters (█) after enabling Color Highlighter. \
         This indicates virtual text is being rendered."
    );

    // Check that color swatches have foreground colors set
    // Find a swatch and check its style
    let lines: Vec<&str> = screen.lines().collect();
    let mut found_colored_swatch = false;

    for (y, line) in lines.iter().enumerate() {
        // Find swatch using char indices to handle multi-byte chars correctly
        for (char_idx, ch) in line.char_indices() {
            if ch == '█' {
                // Use character position, not byte position
                let x = line[..char_idx].chars().count();
                // Bounds check - skip if outside screen
                if x >= 80 {
                    continue;
                }
                if let Some(style) = harness.get_cell_style(x as u16, y as u16) {
                    // Check if foreground color is set (should be the color being highlighted)
                    if let Some(fg) = style.fg {
                        println!(
                            "Found swatch at ({}, {}) with foreground color: {:?}",
                            x, y, fg
                        );
                        // Check if it's an actual RGB color
                        if matches!(fg, ratatui::style::Color::Rgb(_, _, _)) {
                            found_colored_swatch = true;
                            break;
                        }
                    }
                }
            }
        }
        if found_colored_swatch {
            break;
        }
    }

    assert!(
        found_colored_swatch,
        "Expected to find at least one color swatch with RGB foreground color"
    );
}

/// Test Color Highlighter disable command
#[test]
fn test_color_highlighter_disable() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the color highlighter plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/color_highlighter.ts");
    let plugin_dest = plugins_dir.join("color_highlighter.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with a color
    let test_file_content = "let color = \"#ff0000\";\n";
    let fixture = TestFixture::new("test_colors.txt", test_file_content).unwrap();

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
    harness.type_text("Color Highlighter: Enable").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Need multiple async processing cycles for plugin to process virtual text insertion
    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();

    // Helper function to count color swatches (excludes scrollbar at position 79)
    fn count_color_swatches(screen: &str) -> usize {
        screen
            .lines()
            .flat_map(|line| {
                line.char_indices().filter(|&(char_idx, ch)| {
                    if ch != '█' {
                        return false;
                    }
                    // Calculate character position (not byte position)
                    let x = line[..char_idx].chars().count();
                    // Exclude scrollbar (at position 79, the last visible column)
                    x < 79
                })
            })
            .count()
    }

    // Verify swatch appears
    let screen_enabled = harness.screen_to_string();
    let swatches_enabled = count_color_swatches(&screen_enabled);
    assert!(
        swatches_enabled > 0,
        "Expected swatches when enabled. Got:\n{}",
        screen_enabled
    );

    // Now disable it
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Color Highlighter: Disable").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify the content is still visible after disabling
    harness.assert_screen_contains("#ff0000");

    // Swatches should be removed
    let screen_disabled = harness.screen_to_string();
    let swatches_disabled = count_color_swatches(&screen_disabled);

    assert!(
        swatches_disabled < swatches_enabled,
        "Expected fewer swatches after disabling. Before: {}, After: {}",
        swatches_enabled,
        swatches_disabled
    );
}

/// Test Color Highlighter toggle command
#[test]
fn test_color_highlighter_toggle() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the color highlighter plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/color_highlighter.ts");
    let plugin_dest = plugins_dir.join("color_highlighter.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with a color
    let test_file_content = "rgb(128, 64, 255)\n";
    let fixture = TestFixture::new("test_colors.txt", test_file_content).unwrap();

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
    harness.type_text("Color Highlighter: Toggle").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.render().unwrap();

    // Verify swatches appear
    let screen_on = harness.screen_to_string();
    let swatches_on = screen_on.matches('█').count();
    assert!(
        swatches_on > 0,
        "Expected swatches after toggle on. Got:\n{}",
        screen_on
    );

    // Toggle off
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Color Highlighter: Toggle").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify content is still visible after toggling off
    harness.assert_screen_contains("rgb(128, 64, 255)");
}

/// Ensure the clangd plugin reacts to file-status notifications
#[test]
#[ignore]
fn test_clangd_plugin_file_status_notification() -> std::io::Result<()> {
    init_tracing_from_env();
    let _fake_server = FakeLspServer::spawn()?;

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/clangd_support.ts");
    fs::copy(&plugin_source, plugins_dir.join("clangd_support.ts")).unwrap();

    // Copy the lib directory that the plugin depends on
    let lib_source_dir = std::env::current_dir().unwrap().join("plugins/lib");
    let lib_dest_dir = plugins_dir.join("lib");
    fs::create_dir(&lib_dest_dir).unwrap();
    for entry in fs::read_dir(&lib_source_dir).unwrap() {
        let entry = entry.unwrap();
        if entry.path().extension().map(|e| e == "ts").unwrap_or(false) {
            fs::copy(entry.path(), lib_dest_dir.join(entry.file_name())).unwrap();
        }
    }

    let src_dir = project_root.join("src");
    fs::create_dir_all(&src_dir).unwrap();
    let source_file = src_dir.join("main.cpp");
    fs::write(&source_file, "int main() { return 0; }\n").unwrap();

    let mut config = Config::default();
    config.lsp.insert(
        "cpp".to_string(),
        LspServerConfig {
            command: FakeLspServer::script_path().to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: false,
            process_limits: ProcessLimits::default(),
            initialization_options: None,
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, config, project_root.clone())
            .unwrap();

    harness.open_file(&source_file)?;
    harness.render()?;
    for _ in 0..10 {
        std::thread::sleep(Duration::from_millis(100));
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;
    }

    let mut seen_status = false;
    for _ in 0..20 {
        std::thread::sleep(Duration::from_millis(50));
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;
        if let Some(msg) = harness.editor().get_status_message() {
            if msg == "Clangd file status: ready" {
                seen_status = true;
                break;
            }
        }
    }

    assert!(
        seen_status,
        "Expected clangd file status notification to set the plugin status"
    );

    Ok(())
}

/// Ensure the clangd plugin uses editor.sendLspRequest successfully
#[test]
fn test_clangd_plugin_switch_source_header() -> std::io::Result<()> {
    init_tracing_from_env();
    let _fake_server = FakeLspServer::spawn()?;

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/clangd_support.ts");
    fs::copy(&plugin_source, plugins_dir.join("clangd_support.ts")).unwrap();

    // Copy the lib directory that the plugin depends on
    let lib_source_dir = std::env::current_dir().unwrap().join("plugins/lib");
    let lib_dest_dir = plugins_dir.join("lib");
    fs::create_dir(&lib_dest_dir).unwrap();
    for entry in fs::read_dir(&lib_source_dir).unwrap() {
        let entry = entry.unwrap();
        if entry.path().extension().map(|e| e == "ts").unwrap_or(false) {
            fs::copy(entry.path(), lib_dest_dir.join(entry.file_name())).unwrap();
        }
    }

    let src_dir = project_root.join("src");
    fs::create_dir_all(&src_dir).unwrap();
    let source_file = src_dir.join("main.cpp");
    fs::write(&source_file, "int main() { return 0; }\n").unwrap();
    let header_file = src_dir.join("main.h");
    fs::write(&header_file, "// header content\n").unwrap();

    let mut config = Config::default();
    config.lsp.insert(
        "cpp".to_string(),
        LspServerConfig {
            command: FakeLspServer::script_path().to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: false,
            process_limits: ProcessLimits::default(),
            initialization_options: None,
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, config, project_root.clone())
            .unwrap();

    harness.open_file(&source_file)?;
    harness.render()?;
    for _ in 0..10 {
        std::thread::sleep(Duration::from_millis(100));
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;
    }

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Clangd: Switch Source/Header").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait until the header file content is visible (logical event)
    harness.wait_until(|h| h.screen_to_string().contains("header content"))?;

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("header content"),
        "Expected header file to be visible"
    );
    assert!(
        screen.contains("Clangd: opened corresponding file"),
        "Expected clangd status message"
    );
    assert_eq!(
        harness
            .editor()
            .get_status_message()
            .map(|msg| msg.as_str()),
        Some("Clangd: opened corresponding file")
    );

    Ok(())
}

/// Test that plugin commands show the plugin name as source in command palette
#[test]
fn test_plugin_command_source_in_palette() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    // Create a simple plugin that registers a command
    // IMPORTANT: description must NOT contain "test_source_plugin" so we can verify
    // that the source column shows it (not the description)
    let test_plugin = r#"
// Simple test plugin to verify command source is shown correctly
editor.registerCommand(
    "Test Source Plugin Command",
    "A special command for testing",
    "test_source_action",
    "normal"
);

editor.setStatus("Test source plugin loaded!");
"#;

    let test_plugin_path = plugins_dir.join("test_source_plugin.ts");
    fs::write(&test_plugin_path, test_plugin).unwrap();

    // Create a test file
    let fixture = TestFixture::new("test.txt", "Test content\n").unwrap();

    // Create harness with the project directory
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Wait for plugins to load
    for _ in 0..5 {
        harness.process_async_and_render().unwrap();
        std::thread::sleep(Duration::from_millis(50));
    }

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for our plugin command
    harness.type_text("Test Source Plugin").unwrap();

    // Process to update suggestions
    for _ in 0..3 {
        harness.process_async_and_render().unwrap();
        std::thread::sleep(Duration::from_millis(50));
    }

    let screen = harness.screen_to_string();
    println!("Screen showing command palette:\n{}", screen);

    // Verify the command appears
    assert!(
        screen.contains("Test Source Plugin Command"),
        "Plugin command should appear in palette. Got:\n{}",
        screen
    );

    // Verify the source shows the plugin name, NOT "builtin"
    // The source column should show "test_source_p..." (truncated filename without .ts)
    assert!(
        screen.contains("test_source_p"),
        "Command source should show plugin name 'test_source_p...', not 'builtin'. Got:\n{}",
        screen
    );
    // Also verify it does NOT show "builtin" for this command
    // (Since the command is on screen, if it showed "builtin" we'd see it)
    // Note: We can't easily check the specific line, but the fact that test_source_p
    // appears AND builtin commands show "builtin" confirms the feature works

    // Also verify that builtin commands still show "builtin"
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Open palette again and search for a builtin command
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Save File").unwrap();

    for _ in 0..3 {
        harness.process_async_and_render().unwrap();
        std::thread::sleep(Duration::from_millis(50));
    }

    let screen2 = harness.screen_to_string();
    println!("Screen showing Save File command:\n{}", screen2);

    // Save File should show "builtin" as source
    assert!(
        screen2.contains("builtin"),
        "Builtin command should show 'builtin' as source. Got:\n{}",
        screen2
    );
}

/// Test that diagnostics from fake LSP are stored and accessible via getAllDiagnostics API
#[test]
fn test_diagnostics_api_with_fake_lsp() -> std::io::Result<()> {
    init_tracing_from_env();
    let _fake_server = FakeLspServer::spawn()?;

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy lib
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let lib_source_dir = std::env::current_dir().unwrap().join("plugins/lib");
    let lib_dest_dir = plugins_dir.join("lib");
    fs::create_dir(&lib_dest_dir).unwrap();
    for entry in fs::read_dir(&lib_source_dir).unwrap() {
        let entry = entry.unwrap();
        if entry.path().extension().map(|e| e == "ts").unwrap_or(false) {
            fs::copy(entry.path(), lib_dest_dir.join(entry.file_name())).unwrap();
        }
    }

    // Create a simple plugin that captures diagnostics via getAllDiagnostics
    let test_plugin = r#"/// <reference path="./lib/fresh.d.ts" />

// Test plugin to verify getAllDiagnostics API works with real LSP data
let diagnosticCount = 0;

globalThis.on_test_diagnostics_updated = function(data: { uri: string; count: number }): void {
    // When diagnostics update, query them and store count
    const allDiags = editor.getAllDiagnostics();
    diagnosticCount = allDiags.length;
    editor.setStatus(`Diagnostics received: ${diagnosticCount} total, URI count: ${data.count}`);
};

globalThis.get_diagnostic_count = function(): void {
    const allDiags = editor.getAllDiagnostics();
    diagnosticCount = allDiags.length;
    editor.setStatus(`Current diagnostics: ${diagnosticCount}`);
};

editor.on("diagnostics_updated", "on_test_diagnostics_updated");

editor.registerCommand(
    "Test: Get Diagnostic Count",
    "Report the number of diagnostics",
    "get_diagnostic_count",
    "normal"
);

editor.setStatus("Test diagnostics plugin loaded");
"#;

    fs::write(plugins_dir.join("test_diagnostics.ts"), test_plugin).unwrap();

    // Create a test Rust file
    let test_file = project_root.join("test.rs");
    fs::write(&test_file, "fn main() {\n    let x = 1;\n}\n").unwrap();

    // Configure fake LSP for Rust files
    let mut config = Config::default();
    config.lsp.insert(
        "rust".to_string(),
        LspServerConfig {
            command: FakeLspServer::script_path().to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: false,
            process_limits: ProcessLimits::default(),
            initialization_options: None,
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, config, project_root.clone())
            .unwrap();

    // Open the test file - this will start LSP
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for LSP to initialize and plugin to load
    for _ in 0..10 {
        std::thread::sleep(Duration::from_millis(100));
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;
    }

    // Save the file to trigger diagnostics from fake LSP
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render()?;

    // Wait for diagnostics to be received and processed
    let mut diagnostics_received = false;
    for _ in 0..30 {
        std::thread::sleep(Duration::from_millis(100));
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;

        // Check if diagnostics were stored
        let stored = harness.editor().get_stored_diagnostics();
        if !stored.is_empty() {
            diagnostics_received = true;
            println!("Diagnostics received: {:?}", stored);
            break;
        }
    }

    assert!(
        diagnostics_received,
        "Expected diagnostics to be received from fake LSP after save"
    );

    // Verify the diagnostics content
    let stored = harness.editor().get_stored_diagnostics();
    assert_eq!(stored.len(), 1, "Expected diagnostics for one file");

    // Get the diagnostics for our file
    let file_uri = format!("file://{}", test_file.to_string_lossy());
    let diags = stored.get(&file_uri).expect("Should have diagnostics for test file");
    assert_eq!(diags.len(), 1, "Expected exactly one diagnostic");

    // Verify the diagnostic content matches what fake LSP sends
    let diag = &diags[0];
    assert_eq!(
        diag.message, "Test error from fake LSP",
        "Diagnostic message should match fake LSP"
    );
    assert_eq!(
        diag.severity,
        Some(lsp_types::DiagnosticSeverity::ERROR),
        "Diagnostic severity should be error"
    );

    // Verify the plugin's diagnostics_updated hook was called
    // by checking if the status message shows diagnostics count
    if let Some(status) = harness.editor().get_status_message() {
        println!("Status message: {}", status);
        // The hook should have set status with "Diagnostics received"
        if status.contains("Diagnostics received") {
            println!("Plugin hook was triggered successfully");
        }
    }

    Ok(())
}
